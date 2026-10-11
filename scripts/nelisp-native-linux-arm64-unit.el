;;; nelisp-native-linux-arm64-unit.el --- Reader owned cache maintenance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; AGENTS.md clause 3: privileged cache/memory instructions have no Lisp form.
;; Called through the existing ptr-call/native-symbol-address FFI boundary.
(require 'nelisp-asm-arm64)
(require 'nelisp-static-linker)
(defun nelisp-native-linux-arm64-publish-unit ()
  "Build the immutable reader helper for [START, END) cache publication.
CTR_EL0 supplies independent D/I line sizes. IDC and DIC may safely be ignored:
performing the full maintenance sequence is valid on those CPUs as well."
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-cmp-reg-reg b 'x0 'x1)
    (nelisp-asm-arm64-b-cond b 'hs 'done)
    (nelisp-asm-arm64--emit-word b #xd53b0022) ; MRS x2, CTR_EL0
    ;; DminLine (bits 19:16), 4 << DminLine, align START down.
    (nelisp-asm-arm64-mov-imm64 b 'x3 16)
    (nelisp-asm-arm64-lsrv b 'x3 'x2 'x3)
    (nelisp-asm-arm64-mov-imm64 b 'x4 15)
    (nelisp-asm-arm64-and-reg-reg b 'x3 'x3 'x4)
    (nelisp-asm-arm64-mov-imm64 b 'x4 4)
    (nelisp-asm-arm64-lslv b 'x4 'x4 'x3)
    (nelisp-asm-arm64-sub-reg-reg b 'x3 'xzr 'x4)
    (nelisp-asm-arm64-and-reg-reg b 'x3 'x0 'x3)
    (nelisp-asm-arm64-define-label b 'dc-loop)
    (nelisp-asm-arm64--emit-word b #xd50b7b23) ; DC CVAU, x3
    (nelisp-asm-arm64-add-reg-reg b 'x3 'x3 'x4)
    (nelisp-asm-arm64-cmp-reg-reg b 'x3 'x1)
    (nelisp-asm-arm64-b-cond b 'lo 'dc-loop)
    (nelisp-asm-arm64--emit-word b #xd5033b9f) ; DSB ISH
    ;; IminLine (bits 3:0), independent alignment.
    (nelisp-asm-arm64-mov-imm64 b 'x4 15)
    (nelisp-asm-arm64-and-reg-reg b 'x3 'x2 'x4)
    (nelisp-asm-arm64-mov-imm64 b 'x4 4)
    (nelisp-asm-arm64-lslv b 'x4 'x4 'x3)
    (nelisp-asm-arm64-sub-reg-reg b 'x3 'xzr 'x4)
    (nelisp-asm-arm64-and-reg-reg b 'x3 'x0 'x3)
    (nelisp-asm-arm64-define-label b 'ic-loop)
    (nelisp-asm-arm64--emit-word b #xd50b7523) ; IC IVAU, x3
    (nelisp-asm-arm64-add-reg-reg b 'x3 'x3 'x4)
    (nelisp-asm-arm64-cmp-reg-reg b 'x3 'x1)
    (nelisp-asm-arm64-b-cond b 'lo 'ic-loop)
    (nelisp-asm-arm64--emit-word b #xd5033b9f) ; DSB ISH
    (nelisp-asm-arm64--emit-word b #xd5033fdf) ; ISB
    (nelisp-asm-arm64-define-label b 'done)
    (nelisp-asm-arm64-mov-imm64 b 'x0 0)
    (nelisp-asm-arm64-ret b)
    (let* ((text (nelisp-asm-arm64-resolve-fixups b))
           (u (nelisp-link-unit-make "linux-arm64-publish.o" (list (cons 'text text))
                                    (list (nelisp-link-symbol "nl_native_linux_arm64_publish" 0
                                           :section 'text :bind 'global :type 'func)) nil)))
      (plist-put u :machine 'aarch64) u)))
(provide 'nelisp-native-linux-arm64-unit)
