;;; nelisp-standalone-arena-rewrite.el --- Shared compiler arena source rewrite -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Canonical target selection and metadata rewrite shared by AOT and standalone.
;; Loading this module does not load the standalone build orchestration.
;;; Code:
(require 'cl-lib)

(defconst nelisp-standalone--target-aliases
  '(("macos-arm64"   . "macos-aarch64")
    ("linux-arm64"   . "linux-aarch64")
    ("windows-arm64" . "windows-aarch64"))
  "Accepted target spellings that are not this file's canonical names.
The repository spells the Apple-silicon target two ways and they did not
meet: `tools/build-release-artifact.sh', `release/v1.3.0/MACOS-
QUALIFICATION.md' §4 and `nelisp-integration-release-artifact-platforms'
all say `macos-arm64', while every `pcase' arm in this file says
`macos-aarch64'.  Passing the release script's spelling through
NELISP_STANDALONE_TARGET therefore aborted the build with
`standalone: unsupported target macos-arm64' and exit 255 -- which reads
like an unported platform rather than a misspelling.  Measured 2026-09-12
on macos 26.6.2 arm64 while working the v1.3.0 qualification sheet.
Normalising here beats adding a fourth spelling: the canonical names stay
the ones the `pcase' arms already use, and a caller may write either.")

(defconst nelisp-standalone--target
  (intern (let ((raw (or (getenv "NELISP_STANDALONE_TARGET") "linux-x86_64")))
            (or (cdr (assoc raw nelisp-standalone--target-aliases)) raw)))
  "Standalone output target.
Defaults to `linux-x86_64' for backwards compatibility.  Windows-native builds
must opt in with NELISP_STANDALONE_TARGET=windows-x86_64 so Windows-hosted ELF
cache builds do not accidentally mix Win64 units into the SysV cache.
Spellings listed in `nelisp-standalone--target-aliases' are normalised to
the canonical name before interning.")

(defun nelisp-standalone--parse-int-env (name default)
  "Parse integer environment variable NAME, returning DEFAULT when unset.
Accepts decimal strings and 0x-prefixed hexadecimal strings."
  (let ((s (getenv name)))
    (if (and s (> (length s) 0))
        (if (string-match-p "\\`0[xX][0-9a-fA-F]+\\'" s)
            (string-to-number (substring s 2) 16)
          (string-to-number s))
      default)))

(defconst nelisp-standalone--arena-base #x10000000
  "Default standalone arena base used by the Linux/SysV path.")

(defconst nelisp-standalone--windows-arena-base
  (nelisp-standalone--parse-int-env
   "NELISP_STANDALONE_WINDOWS_ARENA_BASE" #x70000000)
  "Windows standalone arena base.
Can be overridden with NELISP_STANDALONE_WINDOWS_ARENA_BASE.  Keep the default
below 2 GiB so Phase47's current signed imm32 materialization remains valid.")

(defconst nelisp-standalone--macos-arena-base #x800000000
  "macOS standalone arena base.
Must live above the 4 GiB __PAGEZERO segment used by the Mach-O executable
writer, and away from the dyld shared cache region used by normal Mach-O
executables.")

(defun nelisp-standalone--target-arena-base (&optional target)
  "Return standalone arena base for TARGET."
  (pcase (or target nelisp-standalone--target)
    ((or 'windows-x86_64 'windows-aarch64) nelisp-standalone--windows-arena-base)
    ('macos-aarch64 nelisp-standalone--macos-arena-base)
    (_ nelisp-standalone--arena-base)))

(defconst nelisp-standalone--arena-rebase-span #x1000
  "Number of low arena-base-relative metadata bytes rewritten per target.")

(defun nelisp-standalone--chunk-arena-rewrite (source)
  "Doc 140 Stage 8: rewrite fixed-arena-base metadata immediates in SOURCE to
load the runtime chunk-0 base from the driver-owned `nl_arena_base' bss slot,
so NO normal runtime path embeds a fixed arena base.

For every chunked native target (linux-x86_64, windows-x86_64,
macos-aarch64), every integer atom N in [target-base, target-base+span) — the
compact metadata block plus the chunk-0 descriptor — becomes `(+ (ptr-read-u64
(data-addr nl_arena_base) 0) OFF)' where OFF = N - target-base.  Because in
the chunked model that whole range is always arena-base-relative (the chunk-0
bump cursor at +0, control slots, the chunk-0 descriptor), the rewrite is
uniform and unambiguous.

The `nl_arena_init' defun is left untouched: it is the one site that reserves
chunk 0 via a NULL-based OS allocation, seeds `nl_arena_base' from the runtime
return value, and may carry reservation SIZE literals that must never be
confused with a fixed base immediate."
  (if (not (memq nelisp-standalone--target
                 '(linux-x86_64 windows-x86_64 macos-aarch64 linux-aarch64
                   windows-aarch64)))
      source
    (let ((base (nelisp-standalone--target-arena-base))
          (span nelisp-standalone--arena-rebase-span))
      (cl-labels
          ((rewrite-int
            (n)
            (if (and (integerp n) (<= base n) (< n (+ base span)))
                `(+ (ptr-read-u64 (data-addr nl_arena_base) 0) ,(- n base))
              n))
           (walk
            (form)
            (cond
             ;; Leave the base-establishing init untouched (mmap(NULL) call +
             ;; SIZE literal live here; the runtime base var does the writes).
             ((and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nl_arena_init))
              form)
             ((integerp form) (rewrite-int form))
             ((consp form) (cons (walk (car form)) (walk (cdr form))))
             (t form))))
        (let ((max-lisp-eval-depth (max max-lisp-eval-depth 10000)))
          (walk source))))))

(defun nelisp-standalone-arena-rewrite-target ()
  "Return the current dynamically scoped standalone target."
  nelisp-standalone--target)

(defun nelisp-standalone-arena-rewrite-base ()
  "Return the historical Linux arena metadata base."
  nelisp-standalone--arena-base)

(defun nelisp-standalone-arena-rewrite-span ()
  "Return the canonical arena metadata rewrite span."
  nelisp-standalone--arena-rebase-span)

(defun nelisp-standalone-arena-rewrite-windows-base ()
  "Return the configured Windows arena metadata base."
  nelisp-standalone--windows-arena-base)

(defun nelisp-standalone-arena-rewrite-target-base (&optional target)
  "Return the canonical metadata base for TARGET or the current target."
  (nelisp-standalone--target-arena-base target))

(defun nelisp-standalone-arena-rewrite-source (source)
  "Rewrite SOURCE with the canonical dynamically scoped target configuration."
  (nelisp-standalone--chunk-arena-rewrite source))

(provide 'nelisp-standalone-arena-rewrite)
;;; nelisp-standalone-arena-rewrite.el ends here
