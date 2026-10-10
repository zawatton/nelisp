;;; windows-native-sentinel.el --- Generated Win64 register probe -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Regenerate: python3 scripts/generate-windows-native-sentinel.py
;; Saves RBX/RBP/RDI/RSI/R12-15 and XMM6-15; forwards all six bridge words.
;; Eight pushes and 216 scratch bytes align RSP and provide 32-byte shadow.
(defconst windows-native-sentinel--code (unibyte-string 85 72 137 229 83 87 86 65 84 65 85 65 86 65 87 72 129 236 216 0 0 0 243 15 127 180 36 48 0 0 0 243 15 127 188 36 64 0 0 0 243 68 15 127 132 36 80 0 0 0 243 68 15 127 140 36 96 0 0 0 243 68 15 127 148 36 112 0 0 0 243 68 15 127 156 36 128 0 0 0 243 68 15 127 164 36 144 0 0 0 243 68 15 127 172 36 160 0 0 0 243 68 15 127 180 36 176 0 0 0 243 68 15 127 188 36 192 0 0 0 72 139 69 48 72 137 68 36 32 72 139 69 56 72 137 68 36 40 72 187 3 119 102 85 68 51 34 17 72 189 5 119 102 85 68 51 34 17 72 191 7 119 102 85 68 51 34 17 72 190 6 119 102 85 68 51 34 17 73 188 12 119 102 85 68 51 34 17 73 189 13 119 102 85 68 51 34 17 73 190 14 119 102 85 68 51 34 17 73 191 15 119 102 85 68 51 34 17 243 15 111 53 159 2 0 0 243 15 111 61 167 2 0 0 243 68 15 111 5 174 2 0 0 243 68 15 111 13 181 2 0 0 243 68 15 111 21 188 2 0 0 243 68 15 111 29 195 2 0 0 243 68 15 111 37 202 2 0 0 243 68 15 111 45 209 2 0 0 243 68 15 111 53 216 2 0 0 243 68 15 111 61 223 2 0 0 72 184 0 0 0 0 0 0 0 0 255 208 72 137 132 36 208 0 0 0 72 184 3 119 102 85 68 51 34 17 72 57 195 15 133 168 1 0 0 72 184 5 119 102 85 68 51 34 17 72 57 197 15 133 149 1 0 0 72 184 7 119 102 85 68 51 34 17 72 57 199 15 133 130 1 0 0 72 184 6 119 102 85 68 51 34 17 72 57 198 15 133 111 1 0 0 72 184 12 119 102 85 68 51 34 17 73 57 196 15 133 92 1 0 0 72 184 13 119 102 85 68 51 34 17 73 57 197 15 133 73 1 0 0 72 184 14 119 102 85 68 51 34 17 73 57 198 15 133 54 1 0 0 72 184 15 119 102 85 68 51 34 17 73 57 199 15 133 35 1 0 0 243 15 111 5 155 1 0 0 102 15 116 198 102 15 215 192 61 255 255 0 0 15 133 8 1 0 0 243 15 111 5 144 1 0 0 102 15 116 199 102 15 215 192 61 255 255 0 0 15 133 237 0 0 0 243 15 111 5 133 1 0 0 102 65 15 116 192 102 15 215 192 61 255 255 0 0 15 133 209 0 0 0 243 15 111 5 121 1 0 0 102 65 15 116 193 102 15 215 192 61 255 255 0 0 15 133 181 0 0 0 243 15 111 5 109 1 0 0 102 65 15 116 194 102 15 215 192 61 255 255 0 0 15 133 153 0 0 0 243 15 111 5 97 1 0 0 102 65 15 116 195 102 15 215 192 61 255 255 0 0 15 133 125 0 0 0 243 15 111 5 85 1 0 0 102 65 15 116 196 102 15 215 192 61 255 255 0 0 15 133 97 0 0 0 243 15 111 5 73 1 0 0 102 65 15 116 197 102 15 215 192 61 255 255 0 0 15 133 69 0 0 0 243 15 111 5 61 1 0 0 102 65 15 116 198 102 15 215 192 61 255 255 0 0 15 133 41 0 0 0 243 15 111 5 49 1 0 0 102 65 15 116 199 102 15 215 192 61 255 255 0 0 15 133 13 0 0 0 72 139 132 36 208 0 0 0 233 10 0 0 0 72 184 239 190 173 222 0 0 0 0 243 15 111 180 36 48 0 0 0 243 15 111 188 36 64 0 0 0 243 68 15 111 132 36 80 0 0 0 243 68 15 111 140 36 96 0 0 0 243 68 15 111 148 36 112 0 0 0 243 68 15 111 156 36 128 0 0 0 243 68 15 111 164 36 144 0 0 0 243 68 15 111 172 36 160 0 0 0 243 68 15 111 180 36 176 0 0 0 243 68 15 111 188 36 192 0 0 0 72 129 196 216 0 0 0 65 95 65 94 65 93 65 92 94 95 91 93 195 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15))
(defconst windows-native-sentinel--hole 308)
(defconst windows-native-sentinel--text-end 897)
(defun windows-native-sentinel-run ()
  (unless (eq system-type 'windows-nt) (error "Win64 sentinel requires Windows"))
  (let* ((original (symbol-function 'nelisp-native-load--symbol-addr))
         (target (funcall original "nl_native_funcall_v2"))
         (size (nelisp-native-load--page-round (length windows-native-sentinel--code)))
         (memory (nelisp-native-load--mmap size nil)))
    (unwind-protect
        (progn
          (nelisp-native-load--poke-string memory 0 windows-native-sentinel--code)
          (ptr-write-u64 memory windows-native-sentinel--hole target)
          (nelisp-native-load--mprotect-rx memory size)
          (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
                     (lambda (name) (if (equal name "nl_native_funcall_v2") memory
                                      (funcall original name)))))
            (f1-root-assert (eq (f1-root-case (lambda () (garbage-collect) 'zero) nil) 'zero) "sentinel N=0")
            (f1-root-assert (= (f1-root-case (lambda (a b c d e f) (garbage-collect) (+ a b c d e f))
                                           '(1 2 3 4 5 6)) 21) "sentinel N=6"))
          (garbage-collect)
          (princ "WINDOWS-REGISTER-SENTINEL-PASS N=0 N=6 GP=8 XMM=10\n"))
      (nelisp-native-load--unmap memory size))))
(defun windows-native-sentinel-with-entry (target callback)
  "Check all Win64 nonvolatile registers around TARGET in CALLBACK."
  (unless (eq system-type 'windows-nt) (error "Win64 sentinel requires Windows"))
  (let* ((call (symbol-function 'ptr-call))
         (size (nelisp-native-load--page-round (length windows-native-sentinel--code)))
         (memory (nelisp-native-load--mmap size nil)))
    (unwind-protect
        (progn
          (nelisp-native-load--poke-string memory 0 windows-native-sentinel--code)
          (ptr-write-u64 memory windows-native-sentinel--hole target)
          (nelisp-native-load--mprotect-rx memory size)
          (cl-letf (((symbol-function 'ptr-call)
                     (lambda (address a b c d e f)
                       (funcall call (if (= address target) memory address) a b c d e f))))
            (funcall callback)))
      (nelisp-native-load--unmap memory size))))
(provide 'windows-native-sentinel)
